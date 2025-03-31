/**
 * Sample TypeScript file for testing ts-morph integration
 */

/**
 * Represents a user in the system
 */
export interface User {
  /** Unique identifier */
  id: number;
  /** User's full name */
  name: string;
  /** Email address (optional) */
  email?: string;
  /** Age in years */
  age: number;
  /** List of permissions */
  permissions: string[];
  /** User status */
  status: 'active' | 'inactive' | 'suspended';
}

/**
 * User service for managing users
 */
export class UserService {
  private users: User[] = [];
  
  /**
   * Add a new user to the system
   */
  addUser(user: User): void {
    this.users.push(user);
  }
  
  /**
   * Get a user by their ID
   */
  getUser(id: number): User | undefined {
    return this.users.find(u => u.id === id);
  }
  
  /**
   * Get all users
   */
  getAllUsers(): User[] {
    return [...this.users];
  }
}

/**
 * User role types
 */
export type UserRole = 'admin' | 'moderator' | 'user' | 'guest';

/**
 * User preferences
 */
export interface UserPreferences {
  theme: 'light' | 'dark' | 'system';
  notifications: boolean;
  language: string;
}

/**
 * Generic pagination result
 */
export interface PaginatedResult<T> {
  items: T[];
  total: number;
  page: number;
  pageSize: number;
  hasMore: boolean;
} 